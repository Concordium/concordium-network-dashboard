@Library('concordium-pipelines') _
pipeline {
    agent any
    environment {
        ecr_repo_domain = '192549843005.dkr.ecr.eu-west-1.amazonaws.com'
        image_repo = "${ecr_repo_domain}/concordium/network-dashboard"
        image_name = "${image_repo}:${image_tag}"
    }
    stages {
        stage('ecr-login') {
            steps {
                ecrLogin(env.ecr_repo_domain, 'eu-west-1')
            }
        }
        stage('build') {
            steps {
                sh '''\
                  docker build -t "${image_name}" -f k8s.Dockerfile .
                  docker push "${image_name}"
                '''
            }
        }
    }
}
