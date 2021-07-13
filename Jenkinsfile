pipeline {
    agent any

    environment {
        image_repo = '192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/network-dashboard'
        image_name = "${image_repo}:${image_tag}"
    }

    stages {
        stage('ecr-login') {
            steps {
                sh '$(aws --region eu-west-1 ecr get-login | sed -e \'s/-e none//g\')'
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
